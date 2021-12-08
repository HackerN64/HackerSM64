#ifndef AUDIO_SEQPLAYER_H
#define AUDIO_SEQPLAYER_H

#include <PR/ultratypes.h>

#include "internal.h"
#include "playback.h"

#define PORTAMENTO_IS_SPECIAL(x) ((x).mode &  0x80)
#define PORTAMENTO_MODE(x)       ((x).mode & ~0x80)
enum PortamentoModes {
    PORTAMENTO_MODE_0,
    PORTAMENTO_MODE_1,
    PORTAMENTO_MODE_2,
    PORTAMENTO_MODE_3,
    PORTAMENTO_MODE_4,
    PORTAMENTO_MODE_5
};

void seq_channel_layer_disable(struct SequenceChannelLayer *seqPlayer);
void sequence_channel_disable(struct SequenceChannel *seqPlayer);
void sequence_player_disable(struct SequencePlayer* seqPlayer);
void audio_list_push_back(struct AudioListItem *list, struct AudioListItem *item);
void *audio_list_pop_back(struct AudioListItem *list);
void process_sequences(s32 iterationsRemaining);
void init_sequence_player(u32 player);
void init_sequence_players(void);

#endif // AUDIO_SEQPLAYER_H
